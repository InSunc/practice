#/bin/python3


"""
You are given two non-empty linked lists representing two non-negative integers. The digits are stored in reverse order, and each of their nodes contains a single digit. Add the two numbers and return the sum as a linked list.

You may assume the two numbers do not contain any leading zero, except the number 0 itself.

Example1:
```
Input: l1 = [2,4,3], l2 = [5,6,4]
Output: [7,0,8]
Explanation: 342 + 465 = 807.
```

Example2:
```
Input: l1 = [0], l2 = [0]
Output: [0]
```

Example3:
```
Input: l1 = [9,9,9,9,9,9,9], l2 = [9,9,9,9]
Output: [8,9,9,9,0,0,0,1]
```

"""

# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, val=0, next=None):
#         self.val = val
#         self.next = next
class Solution:
    def addTwoNumbers(self, l1: Optional[ListNode], l2: Optional[ListNode]) -> Optional[ListNode]:
        current = ListNode()
        head = current

        while l1 != None or l2 != None:
            t1 = ListNode() if l1 == None else l1
            t2 = ListNode() if l2 == None else l2

            current.val += t1.val + t2.val
            print(f'{t1.val} + {t2.val} = {current.val}')
            if current.val >= 10:
                current.val %= 10
                current.next = ListNode(val=1)
            else:
                current.next = ListNode() if (t1.next != None or t2.next != None) else None

            # Advance
            l1 = t1.next
            l2 = t2.next

            # Advance current node
            current = current.next

        return head
